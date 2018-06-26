include Bistro_base.Sigs.DSL
include Bistro_unix__Sigs.S
  with type 'a workflow := 'a workflow
   and type template := Shell_dsl.template
   and type shell_command := shell_command
include Bistro_bioinfo__Sigs.S
  with type 'a workflow := 'a workflow
   and type shell_command := shell_command
   and type docker_image := docker_image

type logger
module Repo : Bistro_base.Sigs.Repo with type 'a workflow := 'a workflow
                                     and type logger := logger
