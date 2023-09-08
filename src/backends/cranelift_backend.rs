use std::path::Path;

use cranelift::prelude::*;
use cranelift_module::{DataDescription, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

use crate::{
    ast::{Expression, UpperStatement},
    error::Result,
    project::Project,
};

use super::Backend;

pub struct CraneliftBackend<'a> {
    project: &'a mut Project,
    builder_context: FunctionBuilderContext,
    ctx: codegen::Context,
    data_description: DataDescription,
    module: ObjectModule,
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

        Self {
            project,
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_description: DataDescription::new(),
            module,
        }
    }

    fn process_expression<'c>(
        &mut self,
        builder: &mut FunctionBuilder<'c>,
        expression: Expression<'b>,
    ) -> Result<Value> {
        let int = self.module.target_config().pointer_type();
        Ok(builder.ins().iconst(int, 12))
    }
}

impl<'a, 'b> Backend<'b> for CraneliftBackend<'a> {
    fn process_upper_statement(&mut self, statement: UpperStatement<'b>) -> Result<()> {
        todo!()
    }

    fn finalize(&mut self, output: &Path, dont_compile: bool) -> Result<()> {
        todo!()
    }

    fn name(&self) -> &'static str {
        todo!()
    }
}
