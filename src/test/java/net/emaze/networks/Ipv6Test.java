package net.emaze.networks;

import net.emaze.dysfunctional.order.Order;
import org.junit.Assert;
import org.junit.Test;

public class Ipv6Test {

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
        Assert.assertEquals(Ipv6.getFirstIp(), Ipv6.getFirstIp().previous());
    }

    @Test
    public void nextIpOfLastIpIsLastIpItSelf() {
        Assert.assertEquals(Ipv6.getLastIp(), Ipv6.getLastIp().next());
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