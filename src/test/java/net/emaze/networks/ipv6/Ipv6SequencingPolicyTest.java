package net.emaze.networks.ipv6;

import java.util.Optional;
import net.emaze.dysfunctional.options.Maybe;
import org.junit.Assert;
import org.junit.Test;

public class Ipv6SequencingPolicyTest {

    private final Ipv6SequencingPolicy instance = new Ipv6SequencingPolicy();

    @Test
    public void thereIsNotNextIpAfterLastOne() {
        Assert.assertEquals(Optional.empty(), instance.next(Ipv6.getLastIp()));
    }

    @Test
    public void canFetchNextIp() {
        Assert.assertEquals(Optional.of(Ipv6.parse("::1")), instance.next(Ipv6.getFirstIp()));
    }
}
