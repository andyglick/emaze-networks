package net.emaze.networks.jackson;

import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import junit.framework.Assert;
import net.emaze.networks.Ipv6;
import net.emaze.networks.jackson.Ipv6ToStringTest.BeanWithIpv6;
import org.junit.Test;

public class Ipv6FromStringTest {

    @Test
    public void deserializingYieldsExpected() throws IOException {
        final String serialized = "{'ipv6':'0000:0000:0000:0000:0000:0000:0000:0001'}".replace("'", "\"");
        final Ipv6 expected = Ipv6.parse("::1");
        final ObjectMapper mapper = new ObjectMapper();
        mapper.registerModule(new NetworksModule());
        final BeanWithIpv6 got = mapper.readValue(serialized, BeanWithIpv6.class);
        Assert.assertEquals(expected, got.getIpv6());
    }
}
