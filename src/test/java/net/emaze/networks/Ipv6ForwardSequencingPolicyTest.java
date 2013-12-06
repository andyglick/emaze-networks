package net.emaze.networks;

import net.emaze.dysfunctional.options.Maybe;
import org.junit.Test;
import org.junit.Assert;

public class Ipv6ForwardSequencingPolicyTest {

    private final Ipv6ForwardSequencingPolicy instance = new Ipv6ForwardSequencingPolicy();

    @Test
    public void thereIsNotNextIpAfterLastOne() {
        Assert.assertEquals(Maybe.nothing(), instance.next(Ipv6.LAST_IP));
    }

    @Test
    public void canFetchNextIp() {
        Assert.assertEquals(Maybe.just(Ipv6.parse("::1")), instance.next(Ipv6.FIRST_IP));
    }
}
