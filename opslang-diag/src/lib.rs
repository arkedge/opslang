use std::cell::RefCell;

pub struct DiagContext {
    emitter: RefCell<Box<dyn DiagEmitter>>,
}

impl DiagContext {
    pub fn new(emitter: Box<dyn DiagEmitter>) -> Self {
        Self {
            emitter: RefCell::new(emitter),
        }
    }

    fn emit_diagnostic(&self, diag: DiagInner) {
        self.emitter.borrow_mut().emit_diagnostic(diag);
    }

    #[track_caller]
    pub fn create_err(&self, err: impl Diagnostic) -> Diag<'_> {
        err.into_diag(self, Level::Error)
    }

    #[track_caller]
    pub fn emit_err(&self, err: impl Diagnostic) {
        self.create_err(err).emit()
    }
}

pub trait DiagEmitter {
    fn emit_diagnostic(&mut self, diag: DiagInner);
}

pub trait Diagnostic {
    fn into_diag(self, dcx: &DiagContext, level: Level) -> Diag<'_>;
}

pub struct DiagInner {
    pub(crate) level: Level,
    pub message: String,
}

impl DiagInner {
    pub fn level(&self) -> &Level {
        &self.level
    }
}

pub enum Level {
    Bug,
    Error,
    Warning,
    Info,
    Lint,
}

pub struct Diag<'dcx> {
    pub dcx: &'dcx DiagContext,
    inner: Option<Box<DiagInner>>,
}

impl Drop for Diag<'_> {
    fn drop(&mut self) {
        if let Some(diag) = self.inner.take() {
            self.dcx.emit_diagnostic(DiagInner::new(
                Level::Bug,
                String::from("the following error was constructed but not emitted"),
            ));
            self.dcx.emit_diagnostic(*diag);
            panic!("error was constructed but not emitted");
        }
    }
}

impl Diag<'_> {
    pub fn emit(mut self) {
        self.dcx.emit_diagnostic(*self.inner.take().unwrap());
    }

    pub fn cancel(mut self) {
        self.inner.take();
    }
}

// constructors

impl DiagInner {
    fn new(level: Level, message: String) -> Self {
        Self { level, message }
    }
}

impl<'dcx> Diag<'dcx> {
    #[track_caller]
    pub fn new(dcx: &'dcx DiagContext, level: Level, message: String) -> Self {
        Self::new_diagnostic(dcx, DiagInner::new(level, message))
    }

    #[track_caller]
    pub(crate) fn new_diagnostic(dcx: &'dcx DiagContext, diag: DiagInner) -> Self {
        Self {
            dcx,
            inner: Some(Box::new(diag)),
        }
    }
}
