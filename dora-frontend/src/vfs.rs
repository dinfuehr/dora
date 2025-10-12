use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use crate::sema::ToArcString;

#[derive(Clone)]
pub struct Vfs(Arc<HashMap<PathBuf, Arc<String>>>);

impl Vfs {
    pub fn new() -> Vfs {
        Vfs(Arc::new(HashMap::new()))
    }

    pub fn open_file<T: ToArcString>(self, path: PathBuf, content: T) -> Vfs {
        let mut data = self.clone_inner_data();
        assert!(data.insert(path, content.into()).is_none());
        Vfs(Arc::new(data))
    }

    pub fn update_file<T: ToArcString>(self, path: PathBuf, content: T) -> Vfs {
        let mut data = self.clone_inner_data();
        assert!(data.insert(path, content.into()).is_some());
        Vfs(Arc::new(data))
    }

    pub fn get(&self, path: &PathBuf) -> Option<Arc<String>> {
        self.0.get(path).cloned()
    }

    pub fn close_file(self, path: PathBuf) -> Vfs {
        let mut data = self.clone_inner_data();
        assert!(data.remove(&path).is_some());
        Vfs(Arc::new(data))
    }

    pub fn clone_inner_data(self) -> HashMap<PathBuf, Arc<String>> {
        match Arc::try_unwrap(self.0) {
            Ok(data) => data,
            Err(rc) => rc.as_ref().clone(),
        }
    }
}
