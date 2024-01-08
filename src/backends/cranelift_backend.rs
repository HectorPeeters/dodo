use std::{collections::HashMap, path::Path, process::Command};

use cranelift::{
    codegen::{verify_function, Context},
    prelude::{isa::CallConv, *},
};
use cranelift_module::{FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

use crate::{
    ast::{
        BinaryOperatorType, Expression, FunctionDeclaration, IntegerLiteralExpr, Statement,
        UnaryOperatorType, UpperStatement,
    },
    error::{Error, ErrorType, Result},
    project::{Project, BUILTIN_TYPE_VOID},
    types::TypeId,
};

use super::Backend;

pub struct CraneliftBackend<'a> {
    project: &'a mut Project,
    module: Option<ObjectModule>,
    pointer_type: Type,
    variables: HashMap<String, Variable>,
    functions: HashMap<String, (FuncId, TypeId)>,
    putchar_id: Option<FuncId>,
    emitted_return: bool,
}

impl<'a, 'b> CraneliftBackend<'a> {
    pub fn new(project: &'a mut Project) -> Self {
        let mut flag_builder = settings::builder();
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();

        let builder =
            ObjectBuilder::new(isa, "backend", cranelift_module::default_libcall_names()).unwrap();

        let module = ObjectModule::new(builder);
        let pointer_type = module.target_config().pointer_type();

        Self {
            project,
            module: Some(module),
            pointer_type,
            variables: HashMap::new(),
            functions: HashMap::new(),
            putchar_id: None,
            emitted_return: false,
        }
    }

    fn module(&mut self) -> &mut ObjectModule {
        self.module.as_mut().unwrap()
    }

    fn convert_type(&self, type_id: TypeId) -> Result<Type> {
        let type_val = self.project.types.get(type_id).unwrap();

        Ok(match type_val {
            crate::types::Type::UInt8() => types::I8,
            crate::types::Type::UInt16() => types::I16,
            crate::types::Type::UInt32() => types::I32,
            crate::types::Type::UInt64() => types::I64,
            crate::types::Type::Bool() => types::I8,
            crate::types::Type::Ptr(_) => self.pointer_type,
            crate::types::Type::Void() => unreachable!(),
            crate::types::Type::Struct(_) => todo!(),
        })
    }

    fn process_expression<'c>(
        &mut self,
        builder: &mut FunctionBuilder<'c>,
        expression: Expression<'b>,
    ) -> Result<Value> {
        match expression {
            Expression::BinaryOperator(binop) => {
                let left = self.process_expression(builder, *binop.left)?;
                let right = self.process_expression(builder, *binop.right)?;

                match binop.op_type {
                    BinaryOperatorType::Add => Ok(builder.ins().iadd(left, right)),
                    BinaryOperatorType::Subtract => Ok(builder.ins().isub(left, right)),
                    BinaryOperatorType::Multiply => Ok(builder.ins().imul(left, right)),
                    BinaryOperatorType::Divide => Ok(builder.ins().udiv(left, right)),
                    BinaryOperatorType::Modulo => Ok(builder.ins().urem(left, right)),
                    BinaryOperatorType::ShiftLeft => Ok(builder.ins().ishl(left, right)),
                    BinaryOperatorType::ShiftRight => Ok(builder.ins().ushr(left, right)),
                    BinaryOperatorType::Equal => todo!(),
                    BinaryOperatorType::NotEqual => todo!(),
                    BinaryOperatorType::LessThan => todo!(),
                    BinaryOperatorType::LessThanEqual => todo!(),
                    BinaryOperatorType::GreaterThan => todo!(),
                    BinaryOperatorType::GreaterThanEqual => todo!(),
                    BinaryOperatorType::LogicalOr => todo!(),
                    BinaryOperatorType::LogicalAnd => todo!(),
                }
            }
            Expression::UnaryOperator(unop) => {
                let value = self.process_expression(builder, *unop.expr)?;
                match unop.op_type {
                    UnaryOperatorType::Negate => Ok(builder.ins().ineg(value)),
                    UnaryOperatorType::Ref => todo!(),
                    UnaryOperatorType::Deref => todo!(),
                }
            }
            Expression::FunctionCall(call) => {
                let (func_id, return_type) = if call.name == "putchar" {
                    (self.putchar_id.unwrap(), BUILTIN_TYPE_VOID)
                } else {
                    *self.functions.get(call.name).unwrap()
                };

                let func = self.module().declare_func_in_func(func_id, builder.func);

                let converted_args = call
                    .args
                    .into_iter()
                    .map(|arg| self.process_expression(builder, arg))
                    .collect::<Result<Vec<_>>>()?;

                let call = builder.ins().call(func, &converted_args);
                if return_type != BUILTIN_TYPE_VOID {
                    Ok(builder.inst_results(call)[0])
                } else {
                    Ok(builder.ins().iconst(types::I8, 0))
                }
            }
            Expression::IntegerLiteral(IntegerLiteralExpr {
                value,
                type_id,
                range: _,
            }) => {
                let target_type = self.convert_type(type_id)?;
                let result = builder.ins().iconst(target_type, value as i64);
                Ok(result)
            }
            Expression::BooleanLiteral(bool) => {
                let result = builder.ins().iconst(types::I8, bool.value as i64);
                Ok(result)
            }
            Expression::VariableRef(var_ref) => {
                let variable = *self.variables.get(var_ref.name).unwrap();
                let result = builder.use_var(variable);
                Ok(result)
            }
            Expression::StringLiteral(string) => {
                let data_id = self
                    .module()
                    .declare_data(&format!("{}", string.value), Linkage::Local, false, false)
                    .unwrap();

                let data_value = self.module().declare_data_in_func(data_id, builder.func);

                let result = builder.ins().global_value(self.pointer_type, data_value);
                Ok(result)
            }
            Expression::StructLiteral(_) => todo!(),
            Expression::FieldAccessor(_) => todo!(),
            Expression::Widen(widen) => {
                let value = self.process_expression(builder, *widen.expr)?;
                let target_type = self.convert_type(widen.type_id)?;
                let result = builder.ins().uextend(target_type, value);
                Ok(result)
            }
            Expression::Cast(_) => todo!(),
            Expression::Type(_) => todo!(),
        }
    }

    fn process_statement<'c>(
        &mut self,
        builder: &mut FunctionBuilder<'c>,
        statement: Statement<'b>,
    ) -> Result<()> {
        match statement {
            Statement::Expression(expression) => {
                self.process_expression(builder, expression.expr)?;
                Ok(())
            }
            Statement::Block(block) => {
                for statement in block.children {
                    self.process_statement(builder, statement)?;
                }
                Ok(())
            }
            Statement::Declaration(decl) => {
                let variable = Variable::new(self.variables.len());
                let var_type = self.convert_type(decl.type_id)?;
                builder.declare_var(variable, var_type);

                self.variables.insert(decl.name.to_owned(), variable);
                Ok(())
            }
            Statement::Assignment(assign) => {
                let variable = match assign.left {
                    Expression::VariableRef(var) => *self.variables.get(var.name).unwrap(),
                    _ => todo!(),
                };

                let value = self.process_expression(builder, assign.right)?;
                builder.def_var(variable, value);

                Ok(())
            }
            Statement::While(_) => todo!(),
            Statement::If(_) => todo!(),
            Statement::Return(ret) => {
                let value = self.process_expression(builder, ret.expr)?;
                builder.ins().return_(&[value]);
                self.emitted_return = true;
                Ok(())
            }
        }
    }

    fn process_function_decl(
        &mut self,
        function_decl: FunctionDeclaration<'b>,
        context: &mut Context,
    ) -> Result<FuncId> {
        // Set parameter types
        for param in function_decl.params {
            let param_type = self.convert_type(param.1)?;
            context
                .func
                .signature
                .params
                .push(AbiParam::new(param_type));
        }

        // Set return type
        if function_decl.return_type != BUILTIN_TYPE_VOID {
            let return_type = self.convert_type(function_decl.return_type)?;
            context
                .func
                .signature
                .returns
                .push(AbiParam::new(return_type));
        }

        // Create function declaration
        let id = self
            .module()
            .declare_function(function_decl.name, Linkage::Export, &context.func.signature)
            .unwrap();

        self.functions.insert(
            function_decl.name.to_owned(),
            (id, function_decl.return_type),
        );

        let mut builder_context = FunctionBuilderContext::new();
        let mut builder = FunctionBuilder::new(&mut context.func, &mut builder_context);

        let entry_block = builder.create_block();
        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        self.emitted_return = false;
        self.process_statement(&mut builder, function_decl.body)?;

        if !self.emitted_return {
            builder.ins().return_(&[]);
        }

        builder.finalize();

        let flags = settings::Flags::new(settings::builder());
        let func = &context.func;
        if let Err(errors) = verify_function(func, &flags) {
            println!("{}", func.display());
            panic!("{}", errors);
        }

        println!("{}", func.display());

        self.module().define_function(id, context).unwrap();

        self.module().clear_context(context);

        Ok(id)
    }
}

impl<'a, 'b> Backend<'b> for CraneliftBackend<'a> {
    fn process_upper_statements(&mut self, statements: Vec<UpperStatement<'b>>) -> Result<()> {
        let mut context = self.module().make_context();

        let mut printf_signature = Signature::new(CallConv::SystemV);
        printf_signature.params.push(AbiParam::new(types::I8));

        let printf_id = self
            .module()
            .declare_function("putchar", Linkage::Export, &printf_signature)
            .unwrap();

        self.putchar_id = Some(printf_id);

        for statement in statements {
            match statement {
                UpperStatement::Function(function_decl) => {
                    self.process_function_decl(function_decl, &mut context)?;
                }
                UpperStatement::StructDeclaration(_) => todo!(),
                UpperStatement::ConstDeclaration(_) => todo!(),
                UpperStatement::ExternDeclaration(_) => {}
            }
        }

        Ok(())
    }

    fn finalize(&mut self, output: &Path, dont_compile: bool) -> Result<()> {
        if dont_compile {
            return Ok(());
        }

        let object_file = output.with_extension("o");

        let result = self.module.take().unwrap().finish();
        let text = result.object.write().unwrap();
        std::fs::write(&object_file, text).unwrap();

        let linker_output = Command::new("gcc")
            .args([
                "-o",
                output.to_str().unwrap(),
                object_file.to_str().unwrap(),
                "-no-pie",
                "-g",
            ])
            .output()
            .expect("Failed to execute linker");

        linker_output.status.exit_ok().map_err(|_| {
            Error::new(
                ErrorType::Postprocess,
                format!(
                    "Linking failed\n\n{}",
                    String::from_utf8(linker_output.stderr).unwrap()
                ),
            )
        })?;

        Ok(())
    }

    fn name(&self) -> &'static str {
        "cranelift"
    }
}
