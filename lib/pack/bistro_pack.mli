include Bistro_base.Sigs.S
include Bistro_unix__Sigs.S
  with type 'a workflow := 'a workflow
   and type template := Shell_dsl.template
   and type shell_command := shell_command

type logger
module Repo : Bistro_base.Sigs.Repo with type 'a workflow := 'a workflow
                                     and type logger := logger
