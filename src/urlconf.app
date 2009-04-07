%% @author Tim Watson <watson.timothy@gmail.com>
%% @copyright 2008 author.

{application, urlconf,
 [{description, "URLCONF, a la Django"},
  {vsn, "0.02"},
  {modules, [
    urlconf.application,
    urlconf.gen_regexp,
    urlconf.regex,
    urlconf.server
  ]},
  {registered, [urlconf.server]},
  {mod, {urlconf.application, []}},
  {env, [{conf, "/tmp/conf/URLCONF"}]},     %% retrieved by calling application:get_env/1,2. The values in the application resource file can be overridden by values in a configuration file (see config(4)) or by command line flags (see erl(1)).
  {applications, [kernel, sasl]}]}.
