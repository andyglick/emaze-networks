package net.emaze.networks;

import net.emaze.dysfunctional.Ranges;
import net.emaze.dysfunctional.order.ComparableComparator;

public class Ipv4Ranges extends Ranges<Ip> {

    public Ipv4Ranges() {
        super(new ComparableComparator<Ip>(), new Ipv4ForwardSequencingPolicy(), Ip.FIRST_IP);
    }
}
