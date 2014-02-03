package net.emaze.networks.my;

import net.emaze.dysfunctional.Consumers;
import net.emaze.dysfunctional.ranges.DenseRange;
import org.junit.Test;
import org.junit.Assert;

public class IpToDenseRangeTest {

    @Test
    public void lastElementOfIterationIsLastIp() {
        final Ip firstIp = Ip.parse("127.0.0.0");
        final Ip lastIp = Ip.parse("127.0.0.1");
        final DenseRange<Ip> allSpace = new IpToDenseRange().perform(firstIp, lastIp);
        Assert.assertEquals(true, (allSpace.begin().equals(firstIp)) && (Consumers.last(allSpace).equals(lastIp)));
    }
}
