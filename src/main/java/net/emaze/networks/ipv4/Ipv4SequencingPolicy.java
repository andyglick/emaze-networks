package net.emaze.networks.ipv4;

import java.util.Optional;
import net.emaze.dysfunctional.options.Maybe;
import net.emaze.dysfunctional.order.SequencingPolicy;

public class Ipv4SequencingPolicy implements SequencingPolicy<Ipv4> {

    @Override
    public Optional<Ipv4> next(Ipv4 ip) {
        if (Ipv4.getLastIp().equals(ip)) {
            return Optional.empty();
        }
        return Optional.ofNullable(ip.next());
    }

}
