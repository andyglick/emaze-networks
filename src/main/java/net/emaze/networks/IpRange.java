package net.emaze.networks;

import net.emaze.dysfunctional.Ranges;
import net.emaze.dysfunctional.options.Maybe;
import net.emaze.dysfunctional.order.ComparableComparator;
import net.emaze.dysfunctional.ranges.DenseRange;

public abstract class IpRange {

    private static final Ranges<Ipv4> RANGES = new Ranges<>(new ComparableComparator<Ipv4>(), new Ipv4ForwardSequencingPolicy(), Ipv4.FIRST_IP);

    public static Maybe<Ipv4> nextOf(Ipv4 ipv4) {
        return new OffsetIpv4().perform(ipv4, 1L);
    }

    public static Maybe<Ipv4> previousOf(Ipv4 ipv4) {
        return new OffsetIpv4().perform(ipv4, -1L);
    }

    public static Maybe<Ipv4> offset(Ipv4 baseIp, long offset) {
        return new OffsetIpv4().perform(baseIp, offset);
    }

    public DenseRange<Ipv4> rangeOf(Ipv4 start, Ipv4 end) {
        return (DenseRange<Ipv4>) RANGES.closed(start, end);
    }
}
