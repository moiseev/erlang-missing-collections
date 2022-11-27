-module(emc_ring_buffer).

-export([
    new/1
]).

-record(ring_buffer, {arr, current_index}).
-opaque t() :: #ring_buffer{}.
-export_type([t/0]).

new(Size) ->
    Arr = array:new(Size, [{default, undefined}, {fixed, true}]),
    #ring_buffer{arr = Arr, current_index = 0}.
