package net.emaze.networks;

import net.emaze.dysfunctional.ranges.DenseRange;
import net.emaze.dysfunctional.tuples.Pair;
import org.junit.Assert;
import org.junit.Test;

public class DenseRangeToIpsTest {

    @Test
    public void canExtractBoundariesFromRange() {
        final Ip firstIp = Ip.parse("192.168.0.0");
        final Ip lastIp = Ip.parse("192.168.0.255");
        final Pair<Ip, Ip> expected = Pair.of(firstIp, lastIp);
        final DenseRange<Ip> range = new IpsToDenseRange().perform(firstIp, lastIp);
        Assert.assertEquals(expected, new DenseRangeToIps().perform(range));
    }
    
    @Test
    public void canExtractBoundariesFromHostRange() {
        final Ip address = Ip.parse("192.168.0.0");
        final Pair<Ip, Ip> expected = Pair.of(address, address);
        final DenseRange<Ip> range = new IpsToDenseRange().perform(address, address);
        Assert.assertEquals(expected, new DenseRangeToIps().perform(range));
    }
    
    @Test
    public void canExtractBoundariesFromAllSpaceRange() {
        final Ip firstIp = Ip.parse("0.0.0.0");
        final Ip lastIp = Ip.parse("255.255.255.255");
        final Pair<Ip, Ip> expected = Pair.of(firstIp, lastIp);
        final DenseRange<Ip> range = new IpsToDenseRange().perform(firstIp, lastIp);
        Assert.assertEquals(expected, new DenseRangeToIps().perform(range));
    }
    
}
