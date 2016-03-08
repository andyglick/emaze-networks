package net.emaze.networks.ipv6;

import java.util.Optional;
import net.emaze.dysfunctional.order.SequencingPolicy;

public class Ipv6SequencingPolicy implements SequencingPolicy<Ipv6> {

    @Override
    public Optional<Ipv6> next(Ipv6 ip) {
        if (Ipv6.getLastIp().equals(ip)) {
            return Optional.empty();
        }
        return Optional.ofNullable(ip.next());
    }

}
