include Sigs.Shell_dsl with type command = Workflow.shell_command
                        and type docker_image := Command.docker_image
                        and type 'a dep := 'a Workflow.t
                        and type template = Template_dsl.template
