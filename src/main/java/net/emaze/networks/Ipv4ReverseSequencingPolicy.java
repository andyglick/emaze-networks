package net.emaze.networks;

import net.emaze.dysfunctional.options.Maybe;
import net.emaze.dysfunctional.order.SequencingPolicy;

public class Ipv4ReverseSequencingPolicy implements SequencingPolicy<Ipv4> {

    @Override
    public Maybe<Ipv4> next(Ipv4 element) {
        if (Ipv4.FIRST_IP == element) {
            return Maybe.nothing();
        }
        return Maybe.just(Ipv4.fromLong(element.toLong() - 1));
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof Ipv4ReverseSequencingPolicy;
    }

    @Override
    public int hashCode() {
        return Ipv4ReverseSequencingPolicy.class.hashCode();
    }
}
