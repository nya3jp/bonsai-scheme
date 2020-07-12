#[derive(Debug)]
pub struct Error {
    msg: String,
}

impl Error {
    pub(crate) fn new(msg: String) -> Error {
        Error{msg}
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.msg)
    }
}

impl std::error::Error for Error {}
