import adt.States.State

package object adt {

  implicit class StateOps[S, A](state: State[S, A]) {

    def map[B](f: A => B): State[S, B] = States.map(state)(f)
    def flatMap[B](f: A => State[S, B]): State[S, B] = States.flatMap(state)(f)
    def map2[B, C](s2: State[S, B])(f: (A, B) => C): State[S, C] = States.map2(state, s2)(f)

  }
}
