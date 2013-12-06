package net.emaze.networks;

import net.emaze.dysfunctional.order.Order;
import org.junit.Test;
import org.junit.Assert;

public class Ipv6Test {

    @Test
    public void mapsFromIpv4() {
        final Ip ipv4 = Ip.parse("127.0.0.1");
        final Ipv6 ipv6 = Ipv6.ipv4Mapped(ipv4);
        final Ipv6 expeced = Ipv6.parse("::ffff:7f00:1");
        Assert.assertEquals(expeced, ipv6);
    }

    @Test
    public void nextIp() {
        final Ipv6 ip = Ipv6.parse("aaaa::ffff:7f00:1");
        final Ipv6 nextIp = ip.next();
        final Ipv6 expected = Ipv6.parse("aaaa::ffff:7f00:2");
        Assert.assertEquals(expected, nextIp);
    }

    @Test
    public void previousIp() {
        final Ipv6 ip = Ipv6.parse("aaaa::ffff:7f00:1");
        final Ipv6 previousIp = ip.previous();
        final Ipv6 expected = Ipv6.parse("aaaa::ffff:7f00:0");
        Assert.assertEquals(expected, previousIp);
    }

    @Test
    public void previousIpOfFirstIpIsFirstIpItSelf() {
        Assert.assertEquals(Ipv6.FIRST_IP, Ipv6.FIRST_IP.previous());
    }

    @Test
    public void nextIpOfLastIpIsLastIpItSelf() {
        Assert.assertEquals(Ipv6.LAST_IP, Ipv6.LAST_IP.next());
    }

    @Test
    public void nextIpIsGreaterThanPrevious() {
        final Ipv6 ip = Ipv6.parse("aaaa::ffff:7f00:1");
        final Ipv6 nextIp = ip.next();
        Assert.assertEquals(Order.GT, Order.of(nextIp.compareTo(ip)));
    }

    @Test
    public void previousIpIsSmallerThanNext() {
        final Ipv6 ip = Ipv6.parse("aaaa::ffff:7f00:1");
        final Ipv6 previousIp = ip.previous();
        Assert.assertEquals(Order.LT, Order.of(previousIp.compareTo(ip)));
    }

}
