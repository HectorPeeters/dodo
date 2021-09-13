use crate::instruction::Instruction;
use std::collections::HashMap;
use std::ops::Range;

#[derive(Debug)]
pub struct InstructionStream<R, C> {
    instructions: Vec<Instruction<R, C>>,
    labels: HashMap<usize, usize>,
}

impl<R, C> InstructionStream<R, C> {
    pub fn new() -> Self {
        Self {
            instructions: vec![],
            labels: HashMap::new(),
        }
    }

    pub fn instr(&mut self, instr: Instruction<R, C>) {
        self.instructions.push(instr);
    }

    pub fn label(&mut self) -> usize {
        let label = self.labels.len();
        self.labels.insert(label, self.instructions.len());
        label
    }

    fn shift_labels_up(&mut self, offset: usize, range: Range<usize>) {
        for (key, value) in &mut self.labels {
            if range.contains(key) {
                *value += offset;
            }
        }
    }

    fn shift_labels_down(&mut self, offset: usize, range: Range<usize>) {
        for (key, value) in &mut self.labels {
            if range.contains(key) {
                *value -= offset;
            }
        }
    }
}

impl<R, C> Iterator for InstructionStream<R, C> {
    type Item = Instruction<R, C>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.instructions.len() {
            0 => None,
            _ => Some(self.instructions.remove(0)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generator_new() {
        let stream = InstructionStream::<usize, u32>::new();
        assert_eq!(stream.instructions.len(), 0);
    }

    #[test]
    fn generator_single_instruction() {
        let mut stream = InstructionStream::<usize, u32>::new();
        stream.instr(Instruction::MovImm(0, 12));
        assert_eq!(stream.instructions.len(), 1);
    }

    #[test]
    fn generator_label() {
        let mut stream = InstructionStream::<usize, u32>::new();
        let label1 = stream.label();
        stream.instr(Instruction::MovImm(0, 12));
        let label2 = stream.label();

        assert_eq!(stream.labels.len(), 2);
        assert_eq!(label1, 0);
        assert_eq!(label2, 1);
        assert_eq!(stream.labels[&label1], 0);
        assert_eq!(stream.labels[&label2], 1);
    }

    #[test]
    fn generator_shift_labels() {
        let mut stream = InstructionStream::<usize, u32>::new();
        let label1 = stream.label();
        stream.instr(Instruction::MovImm(0, 12));
        let label2 = stream.label();
        stream.instr(Instruction::MovImm(0, 13));

        assert_eq!(stream.labels[&label1], 0);
        assert_eq!(stream.labels[&label2], 1);

        stream.shift_labels_up(1, 1..stream.labels.len());

        assert_eq!(stream.labels[&label1], 0);
        assert_eq!(stream.labels[&label2], 2);

        stream.shift_labels_up(1, 0..stream.labels.len() - 1);

        assert_eq!(stream.labels[&label1], 1);
        assert_eq!(stream.labels[&label2], 2);

        stream.shift_labels_down(1, 0..stream.labels.len());

        assert_eq!(stream.labels[&label1], 0);
        assert_eq!(stream.labels[&label2], 1);
    }
}
