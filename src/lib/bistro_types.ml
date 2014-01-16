type 'a workflow = 'a Bistro_workflow.t

type 'a directory = [`directory of 'a]
type 'a file = [`file of 'a]
type 'a zip = [`zip of 'a] file
type 'a tgz = [`tgz of 'a] file
type package = [`package] directory
type pdf = [`pdf] file
