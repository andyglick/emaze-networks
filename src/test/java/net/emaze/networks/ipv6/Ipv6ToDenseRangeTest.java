package net.emaze.networks.ipv6;

import net.emaze.dysfunctional.Consumers;
import net.emaze.dysfunctional.ranges.DenseRange;
import org.junit.Assert;
import org.junit.Test;

public class Ipv6ToDenseRangeTest {

    @Test
    public void lastElementOfIterationIsLastIp() {
        final Ipv6 firstIp = Ipv6.parse("::");
        final Ipv6 lastIp = Ipv6.parse("::1");
        final DenseRange<Ipv6> allSpace = new Ipv6ToDenseRange().perform(firstIp, lastIp);
        Assert.assertEquals(true, (allSpace.begin().equals(firstIp)) && (Consumers.last(allSpace).equals(lastIp)));
    }

}
