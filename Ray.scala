case class Ray(origin: Vec3, direction: Vec3) {
    def at(t: Double) = origin + t*direction
}
