package net.emaze.networks;

import net.emaze.dysfunctional.Ranges;
import net.emaze.dysfunctional.order.ComparableComparator;

public class IpRanges extends Ranges<Ip> {

    public IpRanges() {
        super(new ComparableComparator<Ip>(), new IpForwardSequencingPolicy(), Ip.FIRST_IP);
    }
}
