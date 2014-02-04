package net.emaze.networks.old;

import net.emaze.dysfunctional.Ranges;
import net.emaze.dysfunctional.order.ComparableComparator;

public class Ipv4Ranges extends Ranges<Ipv4> {

    public Ipv4Ranges() {
        super(new ComparableComparator<Ipv4>(), new Ipv4ForwardSequencingPolicy(), Ipv4.FIRST_IP);
    }
}
