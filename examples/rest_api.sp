procedure rest_api is

-- A Client Request

-- A Server Response to a Client Request

-- TODO: https://en.wikipedia.org/wiki/Cross-origin_resource_sharing

type response is record
   -- global fields
   response_api_version  : universal_typeless; -- what edition
   hash                  : universal_string;   -- verification message digest
   -- client fields
   response_time         : universal_typeless; -- when the response happened (server clock)
   response_duration     : universal_typeless; -- how long the last round trip took(?)
   network_lag           : universal_typeless; -- estimate on one-way network delay
   request_token         : universal_string;   -- the client token to verify identity
   request_time          : universal_typeless; -- when the request happened (client clock)
   desired_time          : universal_typeless; -- when the client wants the response (priority)
   request_discardable   : boolean;            -- discard if too late
   request_id            : universal_string;   -- unique id to identify duplicate requests
   is_test               : boolean;            -- true if a test
   request               : json_string;        -- the request
   -- server fields
   server_id             : universal_string;   -- which server provided the data
   last_request_duration : universal_typeless; -- last request turnaround
   data_create_time      : universal_typeless; -- how old is the original data
   data_update_time      : universal_typeless; -- how up-to-date this copy is
   error_message         : universal_typeless; -- any errors that happened
   cache_time            : universal_typeless; -- when it was cached
   new_token             : universal_string;   -- replacement token
   response_format       : universal_typeless; -- how the response is encoded
   length                : natural;            -- if an array, how long it is
   response              : json_string;        -- the data
end record;
-- possibly client session

-- API Call Statistics

type call_stats is abstract record
   last_used             : universal_typeless;
   -- response statistics
   total_calls : natural;
   total_time  : natural;
   rotated_total_calls   : natural; -- from yesterday
   rotated_total_time    : natural;
   last_response_time    : float;
   min_response_time     : float;
   max_response_time     : float;
   avg_response_time     : float; -- latest computed average
   -- request statistics
   total_requests : natural;
   total_request_duration  : natural; -- from yesterday
   rotated_total_requests : natural;
   rotated_total_request_duration  : natural;
   last_request_time    : float;
   min_request_time     : float;
   max_request_time     : float;
   avg_requestn_time     : float; -- latest computed average
end record;

begin
 null; -- not complete
end rest_api;

