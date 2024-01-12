#![allow(non_upper_case_globals)]

pub mod app;
pub mod config;
pub mod db;
pub mod dto;
pub mod error;
pub mod http;
pub mod middleware;
pub mod model;
pub mod openapi;
pub mod scope;
pub mod utils;

pub use crate::error::Error; // lets address it as `web_api::Error` or `crate::Error`
