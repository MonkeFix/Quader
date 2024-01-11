#![allow(non_upper_case_globals)]

pub mod db;
pub mod config;
pub mod app;
pub mod error;
pub mod http;
pub mod model;
pub mod utils;
pub mod scope;
pub mod dto;
pub mod middleware;

pub use crate::error::Error as Error; // lets address it as `web_api::Error` or `crate::Error`
