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
gauge(MetricDataList) when is_list(MetricDataList) ->
    NormalizedData = [normalize_metric_data(MetricData) || MetricData <- MetricDataList],
    send({metric, {gauge, NormalizedData}});
gauge(MetricData) when is_tuple(MetricData) ->
    NormalizedData = normalize_metric_data(MetricData),
    send({metric, {gauge, NormalizedData}}).

-spec gauge(metric_name(), metric_value()) -> ok.
gauge(Name, Value) when is_number(Value) ->
    gauge({Name, Value}).

-spec gauge(metric_name(), metric_value(), metric_sample_rate()|metric_tags()) -> ok.
gauge(Name, Value, SampleRateOrTags) when is_number(Value) andalso (is_number(SampleRateOrTags) orelse is_map(SampleRateOrTags)) ->
    gauge({Name, Value, SampleRateOrTags}).

-spec gauge(metric_name(), metric_value(), metric_sample_rate(), metric_tags()) -> ok.
gauge(Name, Value, SampleRate, Tags) when is_number(SampleRate), is_map(Tags) ->
    gauge({Name, Value, SampleRate, Tags}).

-spec counter(metric_data() | [metric_data()]) -> ok.
counter(MetricDataList) when is_list(MetricDataList) ->
    NormalizedData = [normalize_metric_data(MetricData) || MetricData <- MetricDataList],
    send({metric, {counter, NormalizedData}});
counter(MetricData) when is_tuple(MetricData) ->
    NormalizedData = normalize_metric_data(MetricData),
    send({metric, {counter, NormalizedData}}).

-spec counter(metric_name(), metric_value()) -> ok.
counter(Name, Value) when is_number(Value) ->
    counter({Name, Value}).

-spec counter(metric_name(), metric_value(), metric_sample_rate()|metric_tags()) -> ok.
counter(Name, Value, SampleRateOrTags) when is_number(Value) andalso (is_number(SampleRateOrTags) orelse is_map(SampleRateOrTags)) ->
    counter({Name, Value, SampleRateOrTags}).

-spec counter(metric_name(), metric_value(), metric_sample_rate(), metric_tags()) -> ok.
counter(Name, Value, SampleRate, Tags) when is_number(SampleRate), is_map(Tags) ->
    counter({Name, Value, SampleRate, Tags}).

increment(A) ->
    counter(A).
increment(A, B) ->
    counter(A, B).
increment(A, B, C) ->
    counter(A, B, C).
increment(A, B, C, D) ->
    counter(A, B, C, D).

-spec histogram(metric_data() | [metric_data()]) -> ok.
histogram(MetricDataList) when is_list(MetricDataList) ->
    NormalizedData = [normalize_metric_data(MetricData) || MetricData <- MetricDataList],
    send({metric, {histogram, NormalizedData}});
histogram(MetricData) when is_tuple(MetricData) ->
    NormalizedData = normalize_metric_data(MetricData),
    send({metric, {histogram, NormalizedData}}).

-spec histogram(metric_name(), metric_value()) -> ok.
histogram(Name, Value) when is_number(Value) ->
    histogram({Name, Value}).

-spec histogram(metric_name(), metric_value(), metric_sample_rate()|metric_tags()) -> ok.
histogram(Name, Value, SampleRateOrTags) when is_number(Value) andalso (is_number(SampleRateOrTags) orelse is_map(SampleRateOrTags)) ->
    histogram({Name, Value, SampleRateOrTags}).

-spec histogram(metric_name(), metric_value(), metric_sample_rate(), metric_tags()) -> ok.
histogram(Name, Value, SampleRate, Tags) when is_number(SampleRate), is_map(Tags) ->
    histogram({Name, Value, SampleRate, Tags}).


-spec timer(metric_data() | [metric_data()]) -> ok.
timer(MetricDataList) when is_list(MetricDataList) ->
    NormalizedData = [normalize_metric_data(MetricData) || MetricData <- MetricDataList],
    send({metric, {timer, NormalizedData}});
timer(MetricData) when is_tuple(MetricData) ->
    NormalizedData = normalize_metric_data(MetricData),
    send({metric, {timer, NormalizedData}}).

-spec timer(metric_name(), metric_value()) -> ok.
timer(Name, Value) when is_number(Value) ->
    timer({Name, Value}).

-spec timer(metric_name(), metric_value(), metric_sample_rate()|metric_tags()) -> ok.
timer(Name, Value, SampleRateOrTags) when is_number(Value) andalso (is_number(SampleRateOrTags) orelse is_map(SampleRateOrTags)) ->
    timer({Name, Value, SampleRateOrTags}).

-spec timer(metric_name(), metric_value(), metric_sample_rate(), metric_tags()) -> ok.
timer(Name, Value, SampleRate, Tags) when is_number(SampleRate), is_map(Tags) ->
    timer({Name, Value, SampleRate, Tags}).

timing(A) ->
    timer(A).
timing(A, B) ->
    timer(A, B).
timing(A, B, C) ->
    timer(A, B, C).
timing(A, B, C, D) ->
    timer(A, B, C, D).

-spec set(metric_data() | [metric_data()]) -> ok.
set(MetricDataList) when is_list(MetricDataList) ->
    NormalizedData = [normalize_metric_data(MetricData) || MetricData <- MetricDataList],
    send({metric, {set, NormalizedData}});
set(MetricData) when is_tuple(MetricData) ->
    NormalizedData = normalize_metric_data(MetricData),
    send({metric, {set, NormalizedData}}).

-spec set(metric_name(), metric_value()) -> ok.
set(Name, Value) when is_number(Value) ->
    set({Name, Value}).

-spec set(metric_name(), metric_value(), metric_sample_rate()|metric_tags()) -> ok.
set(Name, Value, SampleRateOrTags) when is_number(Value) andalso (is_number(SampleRateOrTags) orelse is_map(SampleRateOrTags)) ->
    set({Name, Value, SampleRateOrTags}).

-spec set(metric_name(), metric_value(), metric_sample_rate(), metric_tags()) -> ok.
set(Name, Value, SampleRate, Tags) when is_number(SampleRate), is_map(Tags) ->
    set({Name, Value, SampleRate, Tags}).

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

normalize_metric_data({Name, Value}) ->
    {Name, Value, 1.0, #{}};
normalize_metric_data({Name, Value, SampleRate}) when is_number(SampleRate) ->
    {Name, Value, SampleRate, #{}};
normalize_metric_data({Name, Value, Tags}) when is_map(Tags) ->
    {Name, Value, 1.0, Tags};
normalize_metric_data({_Name, _Value, _SampleRate, _Tags} = AlreadyNormalized) ->
    AlreadyNormalized.

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
