package net.emaze.networks.ipv4;

import java.util.Optional;
import org.junit.Assert;
import org.junit.Test;

public class Ipv4SequencingPolicyTest {

    @Test
    public void nextYieldsFollowingIp() {
        final Optional<Ipv4> got = new Ipv4SequencingPolicy().next(Ipv4.parse("10.0.0.0"));
        Assert.assertEquals(Ipv4.parse("10.0.0.1"), got.get());
    }

    @Test
    public void nextYieldsNothingAppliedToLastIp() {
        final Optional<Ipv4> got = new Ipv4SequencingPolicy().next(Ipv4.getLastIp());
        Assert.assertFalse(got.isPresent());
    }
}
