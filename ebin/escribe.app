{application, escribe,
  [{description, "a content tracker and recorder"},
   {id, "escribe"},
   {vsn, "0.01.0"},
   {modules, [escribe, escribe_server, escribe_sup, escribe_evt, escribe_db, 
              escribe_internal, escribe_gather, escribe_util, rss]},
   {applications, [kernel, stdlib, xmerl, mnesia, ibrowse]},
   {registered, [escribe_supervisor, escribe_srv, escribe_logger]},
   {mod, {escribe, []}}
  ]}.

