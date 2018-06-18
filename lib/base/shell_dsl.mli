module type Dep = sig
  type 'a t
  type any
  val any : _ t -> any
end

module Make(Dep : Dep) : Sigs.Shell_dsl
  with type fragment = Dep.any Template.t
   and type command = Dep.any Command.t
   and type 'a dep = 'a Dep.t
   and type docker_image = Command.docker_image
