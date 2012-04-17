import java.util.List;
import java.util.ArrayList;

interface Function0<R> { public R apply(); }
interface Function1<A,R> { public R apply(A a); }
interface Function2<A,B,R> { public R apply(A a, B b); }
interface Function3<A,B,C,R> { public R apply(A a, B b, C c); }

//interface Functor<F> {
    //public <A,B> F<B> map(F<A> a, Function1<A,B> f);

interface Functor<F> {
    public <A,B> F<B> map(F<A> fa, Function1<A,B> f);
}

public class ListFunctor implements Functor<List> {

    public <A,B> List<B> map(final List<A> fa, final Function1<A,B> f) {
        List<B> fb = new ArrayList<B>();

        for (A a: fa)
            fb.add(f.apply(a));

        return fb;
    }

    public static void main(String[] args) {
        List<Integer> xs = new ArrayList<Integer>();
        xs.add(11);
        xs.add(13);
        xs.add(17);
        xs.add(19);

        Function1<Integer,String> f = new Function1<Integer,String>() {
            public String apply(Integer a) {
                return a.toString();
            }
        };

        List<String> ys = new ListFunctor().map(xs, f);

        for (String y : ys)
            System.out.println(y);

        return;
    }
}
