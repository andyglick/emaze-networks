package net.emaze.networks;

import junit.framework.Assert;
import net.emaze.dysfunctional.Consumers;
import net.emaze.dysfunctional.ranges.DenseRange;
import org.junit.Test;

public class IpsToDenseRangeTest {

    @Test
    public void lastElementOfIterationIsLastIp() {
        final Ip firstIp = Ip.parse("127.0.0.0");
        final Ip lastIp = Ip.parse("127.0.0.1");
        final DenseRange<Ip> allSpace = new IpsToDenseRange().perform(firstIp, lastIp);
        Assert.assertEquals(true, (allSpace.begin().equals(firstIp)) && (Consumers.last(allSpace).equals(lastIp)));
    }
}
