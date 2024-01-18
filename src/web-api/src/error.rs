use derive_more::Display;
use serde::Serialize;
use std::fmt;
use utoipa::ToSchema;

#[derive(Debug, Display, Copy, Clone, Serialize, ToSchema)]
#[serde(rename_all = "snake_case")]
pub enum Status {
    #[display(fmt = "success")]
    Success,
    #[display(fmt = "failure")]
    Failure,
    #[display(fmt = "error")]
    Error,
}

fn use_display<T, S>(value: &T, serializer: S) -> Result<S::Ok, S::Error>
where
    T: fmt::Display,
    S: serde::Serializer,
{
    serializer.collect_str(value)
}

#[derive(Debug, Serialize, Clone, ToSchema)]
pub struct Response {
    #[serde(serialize_with = "use_display")]
    pub status: Status,
    #[serde(serialize_with = "use_display")]
    pub message: Error,
}

impl fmt::Display for Response {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", serde_json::to_string(&self).unwrap())
    }
}

#[derive(Debug, PartialEq, Display, Clone, ToSchema)]
pub enum Error {
    #[display(fmt = "Server Error. Please try again later")]
    ServerError,
    #[display(fmt = "Email or password is wrong")]
    WrongCredentials,
    #[display(fmt = "User with this email already exists")]
    EmailExist,
    #[display(fmt = "User belonging to this token no longer exists")]
    UserNoLongerExist,
    #[display(fmt = "User with this id does not exist")]
    UserDoesNotExist,
    #[display(fmt = "Password cannot be empty")]
    EmptyPassword,
    #[display(fmt = "Password must not be more than {} characters", _0)]
    ExceededMaxPasswordLength(usize),
    #[display(fmt = "Error while hashing password")]
    HashingError,
    #[display(fmt = "Invalid password hash format")]
    InvalidHashFormat,
    #[display(fmt = "Authentication token is invalid")]
    InvalidToken,
    #[display(fmt = "You are not logged in, please provide token")]
    TokenNotProvided,
    #[display(fmt = "Access token is expired")]
    AccessTokenExpired,
    #[display(fmt = "Refresh token is expired")]
    RefreshTokenExpired,
    #[display(fmt = "You are not allowed to perform this action")]
    PermissionDenied,
    #[display(fmt = "{}", _0)]
    Message(String),
}

impl Error {
    pub fn from_str(e: impl ToString) -> Self {
        Error::Message(e.to_string())
    }
}
