package net.emaze.networks.my;

import net.emaze.dysfunctional.ranges.DenseRange;
import net.emaze.dysfunctional.tuples.Pair;
import org.junit.Test;
import org.junit.Assert;

public class DenseRangeToIpTest {

    @Test
    public void canExtractBoundariesFromRange() {
        final Ip firstIp = Ip.parse("192.168.0.0");
        final Ip lastIp = Ip.parse("192.168.0.255");
        final Pair<Ip, Ip> expected = Pair.of(firstIp, lastIp);
        final DenseRange<Ip> range = new IpToDenseRange().perform(firstIp, lastIp);
        Assert.assertEquals(expected, new DenseRangeToIp().perform(range));
    }
    
    @Test
    public void canExtractBoundariesFromHostRange() {
        final Ip address = Ip.parse("192.168.0.0");
        final Pair<Ip, Ip> expected = Pair.of(address, address);
        final DenseRange<Ip> range = new IpToDenseRange().perform(address, address);
        Assert.assertEquals(expected, new DenseRangeToIp().perform(range));
    }
    
    @Test
    public void canExtractBoundariesFromAllSpaceRange() {
        final Ip firstIp = Ip.parse("0.0.0.0");
        final Ip lastIp = Ip.parse("255.255.255.255");
        final Pair<Ip, Ip> expected = Pair.of(firstIp, lastIp);
        final DenseRange<Ip> range = new IpToDenseRange().perform(firstIp, lastIp);
        Assert.assertEquals(expected, new DenseRangeToIp().perform(range));
    }

    @Test
    public void canExtractBoundariesFromRangeV6() {
        final Ip firstIp = Ip.parse("2001:0DB8:0000:CD31::");
        final Ip lastIp = Ip.parse("2001:0DB8:0000:CD31::FFFF");
        final Pair<Ip, Ip> expected = Pair.of(firstIp, lastIp);
        final DenseRange<Ip> range = new IpToDenseRange().perform(firstIp, lastIp);
        Assert.assertEquals(expected, new DenseRangeToIp().perform(range));
    }
    
    @Test
    public void canExtractBoundariesFromHostRangeV6() {
        final Ip address = Ip.parse("2001:0DB8:0000:CD31::");
        final Pair<Ip, Ip> expected = Pair.of(address, address);
        final DenseRange<Ip> range = new IpToDenseRange().perform(address, address);
        Assert.assertEquals(expected, new DenseRangeToIp().perform(range));
    }
    
    @Test
    public void canExtractBoundariesFromAllSpaceRangeV6() {
        final Ip firstIp = Ip.parse("::");
        final Ip lastIp = Ip.parse("FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF:FFFF");
        final Pair<Ip, Ip> expected = Pair.of(firstIp, lastIp);
        final DenseRange<Ip> range = new IpToDenseRange().perform(firstIp, lastIp);
        Assert.assertEquals(expected, new DenseRangeToIp().perform(range));
    }
}