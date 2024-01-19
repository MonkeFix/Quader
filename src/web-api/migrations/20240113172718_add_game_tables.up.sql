-- Add up migration script here

CREATE TABLE "match" (
  id UUID NOT NULL PRIMARY KEY DEFAULT (uuid_generate_v4()),
  total_players INT4 NOT NULL,
  start_time TIMESTAMP WITH TIME ZONE NOT NULL,
  total_time FLOAT4 NOT NULL,
  game_settings JSONB NOT NULL
);

CREATE TABLE "replay" (
   id UUID NOT NULL PRIMARY KEY DEFAULT (uuid_generate_v4()),
   user_id UUID NOT NULL,
   CONSTRAINT fk_user FOREIGN KEY (user_id) REFERENCES users(id),
   total_moves INT4 NOT NULL,
   replay_data JSONB NOT NULL
);

CREATE TABLE "match_player_stats" (
   id UUID NOT NULL PRIMARY KEY DEFAULT (uuid_generate_v4()),
   match_id UUID NOT NULL,
   CONSTRAINT fk_match FOREIGN KEY (match_id) REFERENCES match(id),
   user_id UUID NOT NULL,
   CONSTRAINT fk_user FOREIGN KEY (user_id) REFERENCES users(id),
   players_time FLOAT4 NOT NULL,
   apm FLOAT4 NOT NULL,
   pps FLOAT4 NOT NULL,
   total_pieces INT4 NOT NULL,
   is_winner BOOL NOT NULL,
   stats JSONB NOT NULL
);
