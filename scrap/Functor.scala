trait Functor[F[_], A] {
    def fmap[B](f: A => B): F[B]
}

implicit def listFunctor[A](xs: List[A]) = new Functor[List, A] {
    def fmap[B](f: A => B) = xs map f
}

implicit def optionFunctor[A](o: Option[A]) = new Functor[Option, A] {
    def fmap[B](f: A => B) = o map f
}

// vim: set ts=4 sw=4 et:
