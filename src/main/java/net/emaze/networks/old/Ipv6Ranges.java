package net.emaze.networks.old;

import net.emaze.dysfunctional.Ranges;
import net.emaze.dysfunctional.order.ComparableComparator;

public class Ipv6Ranges extends Ranges<Ipv6> {

    public Ipv6Ranges() {
        super(new ComparableComparator<Ipv6>(), new Ipv6ForwardSequencingPolicy(), Ipv6.FIRST_IP);
    }
}
