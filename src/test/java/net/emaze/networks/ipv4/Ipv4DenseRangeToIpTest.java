package net.emaze.networks.ipv4;

import net.emaze.dysfunctional.ranges.DenseRange;
import net.emaze.dysfunctional.tuples.Pair;
import org.junit.Assert;
import org.junit.Test;

public class Ipv4DenseRangeToIpTest {

    @Test
    public void canExtractBoundariesFromRange() {
        final Ipv4 firstIp = Ipv4.parse("192.168.0.0");
        final Ipv4 lastIp = Ipv4.parse("192.168.0.255");
        final Pair<Ipv4, Ipv4> expected = Pair.of(firstIp, lastIp);
        final DenseRange<Ipv4> range = new Ipv4ToDenseRange().apply(firstIp, lastIp);
        Assert.assertEquals(expected, new Ipv4DenseRangeToIp().apply(range));
    }

    @Test
    public void canExtractBoundariesFromHostRange() {
        final Ipv4 address = Ipv4.parse("192.168.0.0");
        final Pair<Ipv4, Ipv4> expected = Pair.of(address, address);
        final DenseRange<Ipv4> range = new Ipv4ToDenseRange().apply(address, address);
        Assert.assertEquals(expected, new Ipv4DenseRangeToIp().apply(range));
    }

    @Test
    public void canExtractBoundariesFromAllSpaceRange() {
        final Ipv4 firstIp = Ipv4.parse("0.0.0.0");
        final Ipv4 lastIp = Ipv4.parse("255.255.255.255");
        final Pair<Ipv4, Ipv4> expected = Pair.of(firstIp, lastIp);
        final DenseRange<Ipv4> range = new Ipv4ToDenseRange().apply(firstIp, lastIp);
        Assert.assertEquals(expected, new Ipv4DenseRangeToIp().apply(range));
    }

}
