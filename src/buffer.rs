#[derive(Debug)]
pub struct Buffer<T: Clone + std::fmt::Debug> {
    pub data: Vec<T>,
    pub index: usize,
}

impl<T: Clone + std::fmt::Debug> Buffer<T> {
    pub fn new(data: Vec<T>) -> Self {
        Buffer {data, index: usize::MAX}
    }
    #[inline]
    pub fn _while<F: Fn(T) -> bool>(&mut self, f: F) {
        while self._if(|c| f(c)) {
        }
    }
    #[inline]
    pub fn _if<F: Fn(T) -> bool>(&mut self, f: F) -> bool {
        if self.peek().map_or(false, f) {
            self.index+=1;
            return true;
        }
        return false;
    }
    #[inline]
    pub fn peek(&mut self) -> Option<T> {
        if self.index >= self.data.len()-1 && self.index != usize::MAX {return None}
        Some(self.data[self.index+1].clone())
    }
    #[inline]
    pub fn peek_back(&mut self) -> Option<T> {
        if self.index == 0 || self.index == usize::MAX {return None}
        Some(self.data[self.index-1].clone())
    }
    #[inline]
    pub fn next(&mut self) -> Option<T> {
        let a = self.peek();
        self.index+=1;
        a
    }
    #[inline]
    pub fn current(&mut self) -> Option<T> {
        if self.index >= self.data.len() {return None}
        Some(self.data[self.index].clone())
    }
}
