package net.emaze.networks;

import junit.framework.Assert;
import net.emaze.dysfunctional.Consumers;
import net.emaze.dysfunctional.ranges.DenseRange;
import org.junit.Test;

public class IpsToDenseRangeTest {

    @Test
    public void lastElementOfIterationIsLastIp() {
        final Ipv4 firstIp = Ipv4.parse("127.0.0.0");
        final Ipv4 lastIp = Ipv4.parse("127.0.0.1");
        final DenseRange<Ipv4> allSpace = new IpsToDenseRange().perform(firstIp, lastIp);
        Assert.assertEquals(true, (allSpace.begin().equals(firstIp)) && (Consumers.last(allSpace).equals(lastIp)));
    }
}
