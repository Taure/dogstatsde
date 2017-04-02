-module(dogstatsd).

-type metric_name() :: iodata().
-type metric_value() :: number().
-type metric_type() :: counter | gauge | histogram | timer | set.
-type metric_sample_rate() :: number().
-type metric_tags() :: map().

-type metric_data() :: {metric_name(), metric_value()}
    | {metric_name(), metric_value(), metric_sample_rate()|metric_tags()}
    | {metric_name(), metric_value(), metric_sample_rate(), metric_tags()}.

-type event_title() :: iodata().
-type event_text() :: iodata().
-type event_type() :: info | error | warning | success.
-type event_priority() :: normal | low.
-type event_tags() :: map().

-export_type([
              metric_name/0
             ,metric_value/0
             ,metric_type/0
             ,metric_sample_rate/0
             ,metric_tags/0
             ,metric_data/0
             ]).

-export([
        gauge/1, gauge/2, gauge/3, gauge/4
        ,counter/1 ,counter/2, counter/3, counter/4
        ,increment/1, increment/2, increment/3, increment/4
        ,histogram/1, histogram/2, histogram/3, histogram/4
        ,timer/1, timer/2, timer/3, timer/4
        ,timing/1, timing/2, timing/3, timing/4
        ,set/1, set/2, set/3, set/4
        ,event/1, event/2, event/3, event/4, event/5
        ]).

-spec send_event(event_title(), event_text(), event_type(), event_priority(), event_tags()) -> ok.
send_event(Title, Text, Type, Priority, Tags) ->
    send({event, {Title, Text, Type, Priority, Tags}}).

-spec send({atom(), tuple()}) -> ok.
send(Data) ->
    wpool:cast(dogstatsd_worker, Data).


-spec gauge(metric_data() | [metric_data()]) -> ok.
gauge([]) ->
    ok;
gauge([H|T]) ->
    metric_data(gauge, H),
    gauge(T);
gauge(Data) when is_tuple(Data) ->
    metric_data(gauge, Data).

-spec gauge(metric_name(), metric_value()) -> ok.
gauge(Name, Value) ->
    metric_data(gauge, {Name, Value}).

-spec gauge(metric_name(), metric_value(), metric_sample_rate()) -> ok.
gauge(Name, Value, Rate) ->
    metric_data(gauge, {Name, Value, Rate}).

-spec gauge(metric_name(), metric_value(), metric_sample_rate(), metric_tags()) -> ok.
gauge(Name, Value, Rate, Tags) ->
    metric_data(gauge, {Name, Value, Rate, Tags}).

-spec counter(metric_data() | [metric_data()]) -> ok.
counter([]) ->
    ok;
counter([H|T]) ->
    metric_data(counter, H),
    counter(T);

counter(Data) when is_tuple(Data) ->
    metric_data(counter, Data).

-spec counter(metric_name(), metric_value()) -> ok.
counter(Name, Value) ->
    metric_data(counter, {Name, Value}).

-spec counter(metric_name(), metric_value(), metric_sample_rate()|metric_tags()) -> ok.
counter(Name, Value, Rate) ->
    metric_data(counter, {Name, Value, Rate}).

-spec counter(metric_name(), metric_value(), metric_sample_rate(), metric_tags()) -> ok.
counter(Name, Value, Rate, Tags) ->
    metric_data(counter, {Name, Value, Rate, Tags}).

increment(A) ->
    counter(A).
increment(A, B) ->
    counter(A, B).
increment(A, B, C) ->
    counter(A, B, C).
increment(A, B, C, D) ->
    counter(A, B, C, D).

-spec histogram(metric_data() | [metric_data()]) -> ok.
histogram([]) ->
    ok;
histogram([H|T]) ->
    metric_data(histogram, H),
    histogram(T);
histogram(Data) when is_tuple(Data) ->
    metric_data(histogram, Data).
-spec histogram(metric_name(), metric_value()) -> ok.
histogram(Name, Value) when is_number(Value) ->
    metric_data(histogram, {Name, Value}).

-spec histogram(metric_name(), metric_value(), metric_sample_rate()) -> ok.
histogram(Name, Value, Rate) ->
    metric_data(histogram, {Name, Value, Rate}).

-spec histogram(metric_name(), metric_value(), metric_sample_rate(), metric_tags()) -> ok.
histogram(Name, Value, Rate, Tags) ->
    metric_data(histogram, {Name, Value, Rate, Tags}).

-spec timer(metric_data() | [metric_data()]) -> ok.
timer([]) ->
    ok;
timer([H|T]) ->
    metric_data(timer, H),
    timer(T);
timer(Data) when is_tuple(Data) ->
    metric_data(timer, Data).

-spec timer(metric_name(), metric_value()) -> ok.
timer(Name, Value) ->
    metric_data(timer, {Name, Value}).

-spec timer(metric_name(), metric_value(), metric_sample_rate()) -> ok.
timer(Name, Value, Rate) ->
    metric_data(timer, {Name, Value, Rate}).

-spec timer(metric_name(), metric_value(), metric_sample_rate(), metric_tags()) -> ok.
timer(Name, Value, Rate, Tags) ->
    metric_data(timer, {Name, Value, Rate, Tags}).

timing(A) ->
    timer(A).
timing(A, B) ->
    timer(A, B).
timing(A, B, C) ->
    timer(A, B, C).
timing(A, B, C, D) ->
    timer(A, B, C, D).

-spec set(metric_data() | [metric_data()]) -> ok.
set([]) ->
    ok;
set([H|T]) ->
    metric_data(set, H),
    set(T);
set(Data) when is_tuple(Data) ->
    metric_data(set, Data).

-spec set(metric_name(), metric_value()) -> ok.
set(Name, Value) ->
    metric_data(set, {Name, Value}).

-spec set(metric_name(), metric_value(), metric_sample_rate()) -> ok.
set(Name, Value, Rate) ->
    metric_data(set, {Name, Value, Rate}).

-spec set(metric_name(), metric_value(), metric_sample_rate(), metric_tags()) -> ok.
set(Name, Value, Rate, Tags) ->
    metric_data(set, {Name, Value, Rate, Tags}).

metric_data(Type, {_, _} = Data) ->
    send({metric, {Type, Data}});
metric_data(Type, {_, _, _} = Data) ->
    send({metric, {Type, Data}});
metric_data(Type, {_, _, __, _} = Data) ->
    send({metric, {Type, Data}}).


-spec event(event_title()) -> ok.
event(Title) -> event(Title, "").
-spec event(event_title(), event_text()) -> ok.
event(Title, Text) -> event(Title, Text, info).
-spec event(event_title(), event_text(), event_type()) -> ok.
event(Title, Text, Type) -> event(Title, Text, Type, normal).
-spec event(event_title(), event_text(), event_type(), event_priority()) -> ok.
event(Title, Text, Type, Priority) -> event(Title, Text, Type, Priority, #{}).
-spec event(event_title(), event_text(), event_type(), event_priority(), event_tags()) -> ok.
event(Title, Text, Type, Priority, Tags) ->
    send_event(Title, Text, Type, Priority, Tags).

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%% Tests
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

test_setup() ->
    meck:new(wpool),
    meck:expect(wpool, 'cast', fun (dogstatsd_worker, _Data) -> ok end).

test_teardown(_) ->
    meck:unload(wpool).

gauge_test_() ->
    {setup,
     fun test_setup/0,
     fun test_teardown/1,
     [
      ?_assertEqual(ok, dogstatsd:gauge("foo.bar", 1))
     ,?_assertEqual(ok, dogstatsd:gauge("foo.bar", 1, 0.5))
     ,?_assertEqual(ok, dogstatsd:gauge("foo.bar", 1, #{baz => qux}))
     ,?_assertEqual(ok, dogstatsd:gauge("foo.bar", 1, 0.25, #{baz => qux}))
     ,?_assertError(function_clause, dogstatsd:gauge("foo.bar", #{baz => qux}))
     ,?_assertError(function_clause, dogstatsd:gauge("foo.bar", #{baz => qux}, 0.5))
     ,?_assertError(function_clause, dogstatsd:gauge("foo.bar", 1, "hello"))
     ,?_assertEqual(ok, dogstatsd:gauge([{"foo.bar", 1, 0.5, #{foo => bar}},
                                         {"foo.bar", 1, 0.5, #{foo => bar}}]))
     ,?_assertError(function_clause, dogstatsd:gauge([{"foo.bar", 1, 0.5, #{foo => bar}},
                                                      {"foo.bar", 1, "hello"}]))
     ]}.

normalize_metric_data_test_() ->
    [
     ?_assertEqual({"key", "value", 1.0, #{}}, normalize_metric_data({"key", "value"}))
    ,?_assertEqual({"key", "value", 12, #{}}, normalize_metric_data({"key", "value", 12}))
    ,?_assertEqual({"key", "value", 1.0, #{foo => bar}},
                   normalize_metric_data({"key", "value", #{foo => bar}}))
    ,?_assertEqual({"key", "value", 12, #{foo => bar}},
                   normalize_metric_data({"key", "value", 12, #{foo => bar}}))
    ].

-endif.
