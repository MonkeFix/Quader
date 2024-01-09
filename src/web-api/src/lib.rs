pub mod db;
pub mod config;
pub mod app;
pub mod error;
pub mod model;

pub use crate::error::Error as Error; // lets address it as `web_api::Error` or `crate::Error`
