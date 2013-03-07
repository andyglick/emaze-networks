package net.emaze.networks;

import net.emaze.dysfunctional.options.Maybe;
import net.emaze.dysfunctional.order.SequencingPolicy;

public class Ipv4SequencingPolicy implements SequencingPolicy<Ipv4> {

    @Override
    public Maybe<Ipv4> next(Ipv4 element) {
        if (Ipv4.LAST_IP == element) {
            return Maybe.nothing();
        }
        return Maybe.just(element.next());
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof Ipv4SequencingPolicy;
    }

    @Override
    public int hashCode() {
        return Ipv4SequencingPolicy.class.hashCode();
    }
}
