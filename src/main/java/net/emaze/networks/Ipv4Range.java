package net.emaze.networks;

import net.emaze.dysfunctional.Ranges;
import net.emaze.dysfunctional.order.ComparableComparator;
import net.emaze.dysfunctional.ranges.DenseRange;

public abstract class Ipv4Range {

    private static final Ranges<Ipv4> RANGES = new Ranges<>(new ComparableComparator<Ipv4>(), new Ipv4ForwardSequencingPolicy(), Ipv4.FIRST_IP);

    public static DenseRange<Ipv4> of(Ipv4 first, Ipv4 last) {
        return (DenseRange<Ipv4>) RANGES.closed(first, last);
    }
}
