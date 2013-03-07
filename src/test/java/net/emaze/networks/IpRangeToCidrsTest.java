package net.emaze.networks;

import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import junit.framework.Assert;
import net.emaze.dysfunctional.Ranges;
import net.emaze.dysfunctional.order.ComparableComparator;
import net.emaze.dysfunctional.ranges.DenseRange;
import org.junit.Test;

public class IpRangeToCidrsTest {

    //TODO: extract a glorified factory? Or a façade for IPv4 ranges?
    public static final Ranges RANGES = new Ranges(new ComparableComparator<Ipv4>(), new Ipv4ForwardSequencingPolicy(), Ipv4.FIRST_IP);

    @Test(expected = IllegalArgumentException.class)
    public void callingWithNullArgumentYieldsException() {
        new IpRangeToCidrs().perform(null);
    }
    
    @Test
    public void canTransformAnEmptyRangeToNoCidrs() {
        final List<Cidr> got = new IpRangeToCidrs().perform((DenseRange) RANGES.empty());
        final List<Cidr> expected = Collections.emptyList();
        Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformARangeWhenItCoincidesWithSpanningCidr() {
        final Ipv4 startIp = Ipv4.parse("192.168.0.0");
        final Ipv4 endIp = Ipv4.parse("192.168.0.255");
        final DenseRange<Ipv4> range = (DenseRange) RANGES.closed(startIp, endIp);
        final List<Cidr> got = new IpRangeToCidrs().perform(range);
        final List<Cidr> expected = Arrays.asList(Cidr.parse("192.168.0.0", 24));
        org.junit.Assert.assertEquals(expected, got);
    }
    
    @Test
    public void canTransformARangeWhenRangeIsStartOfSpanningCidr() {
        final Ipv4 startIp = Ipv4.parse("192.168.0.0");
        final Ipv4 endIp = Ipv4.parse("192.168.1.1");
        final DenseRange<Ipv4> range = (DenseRange) RANGES.closed(startIp, endIp);
        final List<Cidr> got = new IpRangeToCidrs().perform(range);
        final List<Cidr> expected = Arrays.asList(
                Cidr.parse("192.168.0.0", 24),
                Cidr.parse("192.168.1.0", 31));
        org.junit.Assert.assertEquals(expected, got);
    }
    
    @Test
    public void canTransformARangeWhenRangeIsEndOfSpanningCidr() {
        final Ipv4 startIp = Ipv4.parse("192.168.0.128");
        final Ipv4 endIp = Ipv4.parse("192.168.1.255");
        final DenseRange<Ipv4> range = (DenseRange) RANGES.closed(startIp, endIp);
        final List<Cidr> got = new IpRangeToCidrs().perform(range);
        final List<Cidr> expected = Arrays.asList(
                Cidr.parse("192.168.0.128", 25),
                Cidr.parse("192.168.1.0", 24));
        org.junit.Assert.assertEquals(expected, got);
    }
    
    @Test
    public void canTransformARangeWhenRangeIsMiddleOfSpanningCidr() {
        final Ipv4 startIp = Ipv4.parse("192.168.0.128");
        final Ipv4 endIp = Ipv4.parse("192.168.1.127");
        final DenseRange<Ipv4> range = (DenseRange) RANGES.closed(startIp, endIp);
        final List<Cidr> got = new IpRangeToCidrs().perform(range);
        final List<Cidr> expected = Arrays.asList(
                Cidr.parse("192.168.0.128", 25),
                Cidr.parse("192.168.1.0", 25));
        org.junit.Assert.assertEquals(expected, got);
    }

    @Test
    public void canTransformAComplexRangeToCidr() throws UnknownHostException {
        final Ipv4 startIp = Ipv4.parse("37.116.130.0");
        final Ipv4 endIp = Ipv4.parse("37.116.191.255");
        final DenseRange<Ipv4> range = (DenseRange) RANGES.closed(startIp, endIp);
        final List<Cidr> got = new IpRangeToCidrs().perform(range);
        final List<Cidr> expected = Arrays.asList(
                new Cidr(Ipv4.parse("37.116.130.0"), Netmask.fromBits(23)),
                new Cidr(Ipv4.parse("37.116.132.0"), Netmask.fromBits(22)),
                new Cidr(Ipv4.parse("37.116.136.0"), Netmask.fromBits(21)),
                new Cidr(Ipv4.parse("37.116.144.0"), Netmask.fromBits(20)),
                new Cidr(Ipv4.parse("37.116.160.0"), Netmask.fromBits(19)));
        org.junit.Assert.assertEquals(expected, got);
    }
}
