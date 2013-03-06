package net.emaze.networks;

import junit.framework.Assert;
import net.emaze.dysfunctional.Ranges;
import net.emaze.dysfunctional.order.ComparableComparator;
import net.emaze.dysfunctional.ranges.DenseRange;
import org.junit.Test;

public class IpRangeToSpanningCidrTest {
    
    //TODO: extract a glorified factory? Or a façade for IPv4 ranges?
    public static final Ranges RANGES = new Ranges(new ComparableComparator<Ipv4>(), new Ipv4SequencingPolicy(), Ipv4.FIRST_IP);
    
    @Test
    public void spanningAValidRangeYieldsExpected() {
        final Cidr expected = new Cidr(Ipv4.parse("37.116.130.0"), Netmask.fromBits(18));
        final DenseRange<Ipv4> range = (DenseRange)RANGES.closed(Ipv4.parse("37.116.130.0"), Ipv4.parse("37.116.191.255"));
        final Cidr spanning = new IpRangeToSpanningCidr().perform(range);
        Assert.assertEquals(expected, spanning);
    }
}
