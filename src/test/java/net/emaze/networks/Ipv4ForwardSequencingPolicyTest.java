package net.emaze.networks;

import junit.framework.Assert;
import net.emaze.dysfunctional.options.Maybe;
import org.junit.Test;

public class Ipv4ForwardSequencingPolicyTest {
    
    @Test
    public void nextYieldsFollowingIp() {
        final Maybe<Ip> got = new Ipv4ForwardSequencingPolicy().next(Ip.parse("10.0.0.0"));
        Assert.assertEquals(Ip.parse("10.0.0.1"), got.value());
    }
    
    @Test
    public void nextYieldsNothingAppliedToLastIp() {
        final Maybe<Ip> got = new Ipv4ForwardSequencingPolicy().next(Ip.LAST_IP);
        Assert.assertFalse(got.hasValue());
    }
}

