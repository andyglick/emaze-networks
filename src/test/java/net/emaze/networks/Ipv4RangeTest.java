package net.emaze.networks;

import junit.framework.Assert;
import net.emaze.dysfunctional.Consumers;
import net.emaze.dysfunctional.ranges.DenseRange;
import net.emaze.dysfunctional.ranges.Range;
import org.junit.Ignore;
import org.junit.Test;

//TODO: Let sbi loose here 
public class Ipv4RangeTest {

    @Test
    @Ignore
    public void canYieldARangeSpanningAllIpv4Space() {
        final Range<Ipv4> allSpace = Ipv4Range.of(Ipv4.FIRST_IP, Ipv4.LAST_IP);
        Assert.assertEquals(true, (allSpace.begin().equals(Ipv4.FIRST_IP)) && (allSpace.end().value().equals(Ipv4.LAST_IP)));
    }

    @Test
    @Ignore
    public void endOfRangeIsLastIp() {
        final Ipv4 firstIp = Ipv4.parse("127.0.0.0");
        final Ipv4 lastIp = Ipv4.parse("127.0.0.1");
        final Range<Ipv4> allSpace = Ipv4Range.of(firstIp, lastIp);
        Assert.assertEquals(true, (allSpace.begin().equals(firstIp)) && (allSpace.end().value().equals(lastIp)));
    }

    @Test
    public void lastElementOfIterationIsLastIp() {
        final Ipv4 firstIp = Ipv4.parse("127.0.0.0");
        final Ipv4 lastIp = Ipv4.parse("127.0.0.1");
        final DenseRange<Ipv4> allSpace = Ipv4Range.of(firstIp, lastIp);
        Assert.assertEquals(true, (allSpace.begin().equals(firstIp)) && (Consumers.last(allSpace).equals(lastIp)));
    }
}