package net.emaze.networks.ipv6;

import net.emaze.dysfunctional.options.Maybe;
import net.emaze.dysfunctional.order.SequencingPolicy;

public class Ipv6SequencingPolicy implements SequencingPolicy<Ipv6> {

    @Override
    public Maybe<Ipv6> next(Ipv6 ip) {
        if (Ipv6.getLastIp().equals(ip)) {
            return Maybe.nothing();
        }
        return Maybe.just(ip.next());
    }

}
