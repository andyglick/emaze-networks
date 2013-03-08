package net.emaze.networks;

import net.emaze.dysfunctional.Ranges;
import net.emaze.dysfunctional.dispatching.delegates.BinaryDelegate;
import net.emaze.dysfunctional.order.ComparableComparator;
import net.emaze.dysfunctional.ranges.Range;

public class SubtractRangeFromRange implements BinaryDelegate<Range<Ipv4>, Range<Ipv4>, Range<Ipv4>> {

    private static final Ranges<Ipv4> RANGES = new Ranges<>(new ComparableComparator<Ipv4>(), new Ipv4ForwardSequencingPolicy(), Ipv4.FIRST_IP);

    @Override
    public Range<Ipv4> perform(Range<Ipv4> minuend, Range<Ipv4> subtrahend) {
        return RANGES.difference(minuend, subtrahend);
    }
}
