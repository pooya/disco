-type host() :: nonempty_string().
-type path() :: nonempty_string().
-type url() :: binary().

% The host portion of a url, if available.
-type url_host() :: host() | none.

-ifdef(namespaced_types).
-type disco_dict() :: dict:dict().
-type disco_dict(T) :: dict:dict(T).
-type disco_gbtree() :: gb_trees:tree().
-type disco_gbset() :: gb_sets:set().
-type disco_queue() :: queue:queue().
-type disco_queue(T) :: queue:queue(T).
-else.
-type disco_dict() :: dict().
-type disco_dict(_) :: dict().
-type disco_gbtree() :: gb_tree().
-type disco_gbset() :: gb_set().
-type disco_queue() :: queue().
-type disco_queue(_) :: queue().
-endif.
