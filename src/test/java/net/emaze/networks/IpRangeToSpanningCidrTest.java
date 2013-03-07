package net.emaze.networks;

import junit.framework.Assert;
import net.emaze.dysfunctional.Ranges;
import net.emaze.dysfunctional.options.Maybe;
import net.emaze.dysfunctional.order.ComparableComparator;
import net.emaze.dysfunctional.order.JustBeforeNothingComparator;
import net.emaze.dysfunctional.ranges.DenseRange;
import net.emaze.dysfunctional.ranges.Range;
import org.junit.Test;

public class IpRangeToSpanningCidrTest {
    
    //TODO: extract a glorified factory? Or a façade for IPv4 ranges?
    public static final Ranges RANGES = new Ranges(new ComparableComparator<Ipv4>(), new Ipv4ForwardSequencingPolicy(), Ipv4.FIRST_IP);
    
    @Test(expected = IllegalArgumentException.class)
    public void nullRangeYieldsException() {
        new IpRangeToSpanningCidr().perform(null);
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void openEndedRangeYieldsException() {
        final DenseRange<Ipv4> openEnded = new DenseRange<>(new Ipv4ForwardSequencingPolicy(), new JustBeforeNothingComparator(new ComparableComparator<Ipv4>()), Range.Endpoint.Include, Ipv4.FIRST_IP, Maybe.<Ipv4>nothing(), Range.Endpoint.Exclude);
        new IpRangeToSpanningCidr().perform(openEnded);
    }
    
    @Test
    public void spanningAValidRangeYieldsExpected() {
        final Cidr expected = new Cidr(Ipv4.parse("37.116.130.0"), Netmask.fromBits(18));
        final DenseRange<Ipv4> range = (DenseRange)RANGES.closed(Ipv4.parse("37.116.130.0"), Ipv4.parse("37.116.191.255"));
        final Cidr spanning = new IpRangeToSpanningCidr().perform(range);
        Assert.assertEquals(expected, spanning);
    }
}
