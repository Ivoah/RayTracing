case class Portal() extends Hittable {

  override def hit(r: Ray, t_min: Double, t_max: Double): Option[Hit] = ???

  override def bounding_box: AABB = ???
}
