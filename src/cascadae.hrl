%% Income message types
-record(cascadae_action, {type, hash, path}).
-record(cascadae_event, {type, hash, data}).
-record(cascadae_auth, {hash, session_id}).

%% Example for all other objects
-record(cascadae_object, {hash, type}).

%% Real objects
-record(cascadae_table, {hash, type}).
-record(cascadae_form, {hash, type}).
