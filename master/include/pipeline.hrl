% A Disco pipeline is a sequence of stages, and a set of inputs to the
% whole sequence.  Each stage is executed by running a number of
% tasks.  The number of tasks in a stage is determined by the labels
% of the inputs to that stage, and the type of label grouping to be
% performed for that stage.

-type stage_name() :: binary().

-type label() :: non_neg_integer().

-type label_grouping() ::
        % A separate task is created for each input to the stage,
        % regardless of the label (typically used in map stages).
        split
      | % Each task consumes all inputs with the same label on the
        % same node (typically used in local-reduce stages).
        group_node_label
      | % Each task consumes all inputs on the same node (typically
        % used in shuffle stages).
        group_node
      | % Each task consumes all inputs with the same label (typically
        % used in partitioned-reduce stages).
        group_label
      | % A single task should consume all inputs, regardless of their
        % location or label (typically used in non-partitioned reduce
        % stages).
        group_all.

-type stage() :: {stage_name(), label_grouping()}.
-type pipeline() :: [stage()].
-type group() :: {label(), url_host()}.

% Each output of a task is identified with an non-negative integer.
% An assumption in Disco (i.e. a constraint that the user must satisfy
% but which Disco does not verify) is that different runs of the same
% task produce the same outputs with the same ids.
-type task_output_id() :: non_neg_integer().

% An input to a task in a pipeline is identified by the task that
% produced it as output, and its sequence number in that task's
% output.  The input to the pipeline itself is not produced by any
% task in that pipeline, and is tagged with a task_id of 'input'.
-type input_id() :: {task_id() | input, task_output_id()}.

% The location of the data of a task input is identified by a url, and
% the Disco node (if any) that hosts that url.
-type data_replica() :: {url(), url_host()}.

% The specification of a task input contains the label for that input,
% its size in bytes, and a list of locations where it can be found.
% If the size is unknown, a size of 0 is used.
-type data_size() :: non_neg_integer().
-type data_spec() :: {data, {label(), data_size(), [data_replica()]}}.

% A directory input is a local 'directory file' i.e. a file that
% contains a listing of local data filenames, their labels and sizes.
% Local files are specified relative to the job-home directory.  It is
% normally provided as input to a task running on the specified host.
% This is an optimization for the case where there are a very large
% number of output files, and is typically used to plumb together a
% stage with a subsequent shuffle.  The list of labels contained in
% the file are also specified in the spec.
-type dir_spec() :: {dir, {host(), url(), [{label(), data_size()}]}}.

-type data_input() :: data_spec() | dir_spec().

% An identifier for a particular task.  The dummy 'input' task is
% considered to have output the pipeline inputs.
-type task_id() :: non_neg_integer().

% A particular task may be run multiple times, on different hosts or
% the same host, to recover from transient faults.  The run-id
% identifies one such run.
-type task_run_id() :: non_neg_integer().

% Each output generated by the worker in a task run (using a message
% in the worker protocol) is of the same form as an input.
-type task_output_type() :: data_input().

% Each output of a task is specified by its id and type.
-type task_output() :: {task_output_id(), task_output_type()}.

% A grouping operation converts task outputs into inputs for tasks in
% the next stage.
-type grouped_output() :: {group(), [{input_id(), data_input()}]}.

% The static specification of a task.
-type task_schedule() :: local | remote | none.
-record(task_spec, {jobname   :: jobname(),
                    stage     :: stage_name(),
                    taskid    :: task_id(),
                    tasknum   :: integer(),
                    grouping  :: label_grouping(),
                    group     :: group(),
                    job_coord :: pid(),
                    jobenvs   :: [{nonempty_string(), string()}],
                    worker    :: binary(),
                    schedule  :: task_schedule(),
                    input     :: [input_id()],
                    save_outputs :: boolean()}).
-type task_spec() :: #task_spec{}.

% Information particular to a specific run of a task.  Each attempt to
% run a task will have a different runid.  The locations for the
% inputs of the task might vary across runs, due to re-runs.  The host
% specifies the pre-assigned host from grouping, if any, for the task
% run.  On re-runs after faults, the host is selected by the
% host-allocator.
-record(task_run, {runid         :: task_run_id(),
                   host          :: url_host(),
                   failed_hosts  :: gb_set(),
                   input         :: [{input_id(), data_input()}]}).
-type task_run() :: #task_run{}.

-type task() :: {task_spec(), task_run()}.

% Some predefined stages.
-define(INPUT, <<"input">>).
-define(REDUCE, <<"reduce">>).
-define(MAP, <<"map">>).
-define(MAP_SHUFFLE, <<"map_shuffle">>).
-define(REDUCE_SHUFFLE, <<"reduce_shuffle">>).

% The submitted pipeline jobpack.
-record(jobinfo, {jobname       :: jobname(),
                  jobfile       :: path(),
                  jobenvs = []  :: [{nonempty_string(), string()}],
                  owner         :: binary(),
                  worker        :: binary(),
                  inputs = []   :: [task_output()],
                  pipeline = [] :: pipeline(),
                  schedule      :: task_schedule(),
                  save_results = false :: boolean(),
                  save_info     :: string()}).
-type jobinfo() :: #jobinfo{}.
