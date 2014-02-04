package net.emaze.networks.old;

import net.emaze.dysfunctional.options.Maybe;
import net.emaze.dysfunctional.order.SequencingPolicy;

public class Ipv6ForwardSequencingPolicy implements SequencingPolicy<Ipv6> {

    @Override
    public Maybe<Ipv6> next(Ipv6 ip) {
        if (Ipv6.LAST_IP.equals(ip)) {
            return Maybe.nothing();
        }
        return Maybe.just(ip.next());
    }

    @Override
    public boolean equals(Object other) {
        return other instanceof Ipv6ForwardSequencingPolicy;
    }

    @Override
    public int hashCode() {
        return Ipv6ForwardSequencingPolicy.class.hashCode();
    }
}
