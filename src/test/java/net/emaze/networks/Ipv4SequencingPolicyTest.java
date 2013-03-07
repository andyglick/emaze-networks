package net.emaze.networks;

import junit.framework.Assert;
import net.emaze.dysfunctional.options.Maybe;
import org.junit.Test;

public class Ipv4SequencingPolicyTest {
    
    @Test
    public void nextYieldsFollowingIp() {
        final Maybe<Ipv4> got = new Ipv4SequencingPolicy().next(Ipv4.parse("10.0.0.0"));
        Assert.assertEquals(Ipv4.parse("10.0.0.1"), got.value());
    }
    
    @Test
    public void nextYieldsNothingAppliedToLastIp() {
        final Maybe<Ipv4> got = new Ipv4SequencingPolicy().next(Ipv4.LAST_IP);
        Assert.assertFalse(got.hasValue());
    }
}

