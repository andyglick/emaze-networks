package net.emaze.networks;

import net.emaze.dysfunctional.Ranges;
import net.emaze.dysfunctional.dispatching.delegates.BinaryDelegate;
import net.emaze.dysfunctional.order.ComparableComparator;
import net.emaze.dysfunctional.ranges.DenseRange;

public class IpsToDenseRange implements BinaryDelegate<DenseRange<Ipv4>, Ipv4, Ipv4> {

    private static final Ranges<Ipv4> RANGES = new Ranges<>(new ComparableComparator<Ipv4>(), new Ipv4ForwardSequencingPolicy(), Ipv4.FIRST_IP);

    @Override
    public DenseRange<Ipv4> perform(Ipv4 first, Ipv4 last) {
        return (DenseRange<Ipv4>) RANGES.closed(first, last);
    }
}
