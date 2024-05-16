-- Add up migration script here

CREATE TYPE user_role AS ENUM ('admin', 'user', 'moderator', 'supporter', 'guest');
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

CREATE TABLE
    "users" (
        id UUID NOT NULL PRIMARY KEY DEFAULT (uuid_generate_v4()),
        username VARCHAR(100) NOT NULL UNIQUE,
        email VARCHAR(255) NOT NULL UNIQUE,
        password_hash VARCHAR(100) NOT NULL,
        role user_role NOT NULL DEFAULT 'user',
        photo VARCHAR NOT NULL DEFAULT 'default.png',
        verified BOOLEAN NOT NULL DEFAULT FALSE,
        created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
        updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW()
    );

CREATE INDEX users_email_idx ON users (email);
