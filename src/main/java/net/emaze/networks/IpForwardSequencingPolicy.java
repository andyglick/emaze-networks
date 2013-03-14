package net.emaze.networks;

import net.emaze.dysfunctional.options.Maybe;
import net.emaze.dysfunctional.order.SequencingPolicy;

public class IpForwardSequencingPolicy implements SequencingPolicy<Ip> {

    @Override
    public Maybe<Ip> next(Ip element) {
        if (Ip.LAST_IP.equals(element)) {
            return Maybe.nothing();
        }
        return Maybe.just(element.next());
    }

    @Override
    public boolean equals(Object obj) {
        return obj instanceof IpForwardSequencingPolicy;
    }

    @Override
    public int hashCode() {
        return IpForwardSequencingPolicy.class.hashCode();
    }
}
